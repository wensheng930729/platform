package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpOutOfStockOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpOutOfStockOrderDetailService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.datadriver.service.ErpStockService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 领料出库明细表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpOutOfStockOrderDetailServiceImpl extends ServiceImpl<ErpOutOfStockOrderDetailMapper, ErpOutOfStockOrderDetail> implements ErpOutOfStockOrderDetailService {


    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;


    /**
     * 保存领料出库
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOutOfStockOrderDetail(AuthPlatformUserInfo userInfo, ErpOutOfStockOrderDetailRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
        BigDecimal number = rq.getNumber();

        ErpOutOfStockOrderDetail order = BeanUtils.copyProperties(rq, ErpOutOfStockOrderDetail.class);
        if (ObjectUtils.isEmpty(rq.getId())) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            order.setModifierId(userId).setModifyTime(time);
        }

        // 保存出库数量到库存表
        Integer productId = rq.getProductId();
        Integer repositoryId = rq.getRepositoryId();
        Integer companyId = rq.getCompanyId();
        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id", companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(existStock)) {
            log.error("保存领料出库明细失败,库存表里没有该产品，不能出库不存在的产品，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "saveOutOfStockOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_NO_P);
        } else {
            // 修改出库数量
            BigDecimal outStockNum = existStock.getOutStockNum();
            BigDecimal stockNum = existStock.getStockNum();
            // 新增
            if (ObjectUtils.isEmpty(rqId)) {
                existStock.setOutStockNum(number.add(outStockNum));
                // 如果出库数量大于 现有量 则不能出库 抛异常
                if (number.compareTo(stockNum) > 0) {
                    log.error("保存领料出库明细失败,库存现有量小于出库数量，不能超额出库产品，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "saveOutOfStockOrderDetail()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_NO_NUM);

                }
                existStock.setStockNum(stockNum.subtract(number));
            } else {
                ErpOutOfStockOrderDetail exist = selectById(rqId);
                if (!ObjectUtils.isEmpty(exist)) {
                    // 编辑
                    BigDecimal oldNumber = exist.getNumber();
                    // 新的减旧的 差值
                    BigDecimal newNumber = number.subtract(oldNumber);
                    // 出库数量 加上差值
                    existStock.setOutStockNum(newNumber.add(outStockNum));
                    // 如果出库数量大于 现有量 则不能出库 抛异常
                    if (newNumber.compareTo(stockNum) > 0) {
                        log.error("保存领料出库明细失败,库存现有量小于出库数量，不能超额出库产品，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "saveOutOfStockOrderDetail()");
                        throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED);

                    }
                    // 现存量 减去 差值
                    existStock.setStockNum(stockNum.subtract(newNumber));
                } else {

                    log.info("保存领料出库明细时，调整库存，找不到相关明细记录");
                }

            }
        }

        if (!erpStockService.updateById(existStock)) {
            log.error("保存领料出库明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "saveOutOfStockOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);
        }


        if (!insertOrUpdate(order)) {
            log.error("保存领料出库明细失败，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "saveOutOfStockOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED);
        }


        return order.getId();
    }

    /**
     * 删除领料出库明细
     *
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteOutOfStockOrderDetailById(AuthPlatformUserInfo userInfo, Integer id) {

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpOutOfStockOrderDetail exist = selectOne(new EntityWrapper<ErpOutOfStockOrderDetail>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("删除领料出库详情时，没有找到相关记录,id为："+id);
            return;
        }

        BigDecimal oldNumber = exist.getNumber();
        Integer productId = exist.getProductId();
        Integer repositoryId = exist.getRepositoryId();
        Integer companyId = exist.getCompanyId();
        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id", companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if (!ObjectUtils.isEmpty(existStock)) {
            // 有库存信息 调整数据
            BigDecimal oldOutStockNum = existStock.getOutStockNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            // 删除  减除old
            existStock.setOutStockNum(oldOutStockNum.subtract(oldNumber));
            // 删除  现有量 加上 old
            existStock.setStockNum(oldStockNum.add(oldNumber));

            if (!erpStockService.updateById(existStock)) {
                log.error("删除领料出库明细失败，调整库存期初数量失败,调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "deleteOutOfStockOrderDetailById()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }
        } else {

            log.info("删除领料出库明细时，调整库存量，没有找到相关记录");
        }

        if (!updateById(new ErpOutOfStockOrderDetail().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除领料出库明细失败，调用{}的{}方法出错", "ErpOutOfStockOrderDetailServiceImpl", "deleteOutOfStockOrderDetailById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DETAIL_DELETE_FAILED);
        }

    }

    /**
     * 根据id查询领料出库详情
     * @param id
     * @return
     */
    @Override
    public ErpOutOfStockOrderDetailDTO getOutOfStockOrderDetailById(Integer id) {
        ErpOutOfStockOrderDetailDTO dto = new ErpOutOfStockOrderDetailDTO();

        ErpOutOfStockOrderDetail d = selectOne(new EntityWrapper<ErpOutOfStockOrderDetail>().eq("id",id).eq("deleted",0));
        if (ObjectUtils.isEmpty(d)) {
            log.info("根据id查看领料出库详情,没有找到相关数据，id为："+id);
            return dto;
        }
        Integer productId = d.getProductId();
        ErpProduct p = productService.selectById(productId);
        if (!ObjectUtils.isEmpty(p)) {
            d.setProductName(p.getName()).setUnit(p.getUnit());
        }

        d.setStorehouse(commonMapper.getRepositoryNameById(d.getRepositoryId()));
        d.setTestReportCode(commonMapper.getTestCodeById(d.getTestReportId()));

        dto = BeanUtils.copyProperties(d, ErpOutOfStockOrderDetailDTO.class);


        return dto;
    }
}
