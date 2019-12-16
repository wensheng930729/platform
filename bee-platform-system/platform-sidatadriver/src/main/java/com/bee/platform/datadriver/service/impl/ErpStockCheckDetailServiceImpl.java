package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpStockCheckDetailMapper;
import com.bee.platform.datadriver.dto.ErpStockCheckDetailDTO;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.entity.ErpStockCheckDetail;
import com.bee.platform.datadriver.rq.ErpStockCheckDetailRQ;
import com.bee.platform.datadriver.service.ErpProductBatchService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.datadriver.service.ErpStockCheckDetailService;
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
 * 库存盘点明细 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpStockCheckDetailServiceImpl extends ServiceImpl<ErpStockCheckDetailMapper, ErpStockCheckDetail> implements ErpStockCheckDetailService {

    @Autowired
    private ErpStockService erpStockService;


    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;


    @Autowired
    private ErpProductBatchService productBatchService;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveStockCheckDetail(AuthPlatformUserInfo userInfo, ErpStockCheckDetailRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
        ErpStockCheckDetail order = BeanUtils.copyProperties(rq, ErpStockCheckDetail.class);
        if (ObjectUtils.isEmpty(rqId)) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            order.setModifierId(userId).setModifyTime(time);
        }

        Integer productId = rq.getProductId();
        Integer productBatchId = rq.getProductBatchId();
        Integer repositoryId = rq.getRepositoryId();
        Integer companyId = rq.getCompanyId();
        BigDecimal expectNumber = rq.getExpectNumber();
        BigDecimal realNumber = rq.getRealNumber();
        BigDecimal dNum = realNumber.subtract(expectNumber);

        Wrapper<ErpStock> wrapper = new EntityWrapper<ErpStock>().eq("org_id", companyId)
                .eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0);
        if(!ObjectUtils.isEmpty(productBatchId)){
            wrapper.eq("product_batch_id", productBatchId);
        }else {
            wrapper.isNull("product_batch_id");
        }
        ErpStock existStock = erpStockService.selectOne(wrapper);
        if (ObjectUtils.isEmpty(existStock)) {
            log.info("保存库存盘点明细失败,库存表里没有该产品，，调用{}的{}方法出错", "ErpStockCheckDetailServiceImpl", "saveStockCheckDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_STOCK_CHECK_DETAIL_SAVE_FAILED_NO_STOCK);

        }
        BigDecimal stockOldDNum = existStock.getDNum();
        BigDecimal oldStockNum = existStock.getStockNum();
        // 如果是新添加
        if (ObjectUtils.isEmpty(rqId)) {

            // 现有量 加上差值
            existStock.setStockNum(oldStockNum.add(dNum));
            // 旧的差值加上新的差值
            existStock.setDNum(stockOldDNum.add(dNum));

        } else {
            // 如果是编辑
            ErpStockCheckDetail exist = selectById(rqId);
            BigDecimal oldExpectNumber = exist.getExpectNumber();
            BigDecimal oldRealNumber = exist.getRealNumber();
            // 旧的详情表里的差值dNum
            BigDecimal eOldDNum = oldRealNumber.subtract(oldExpectNumber);
            // 新的差值和旧的差值的差
            BigDecimal d = dNum.subtract(eOldDNum);

            // 现有量 加上差值
            existStock.setStockNum(oldStockNum.add(d));
            // 旧的差值加上新的差值
            existStock.setDNum(stockOldDNum.add(d));

        }

        if (!erpStockService.updateById(existStock)) {
            log.error("保存库存盘点失败，调整库存盘点调整值，现有量失败,调用{}的{}方法出错", "ErpStockCheckDetailServiceImpl", "saveStockCheckDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_STOCK_CHECK_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);

        }


        if (!insertOrUpdate(order)) {
            log.error("保存库存盘点失败，调用{}的{}方法出错", "ErpStockCheckDetailServiceImpl", "saveStockCheckDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_STOCK_CHECK_DETAIL_SAVE_FAILED);
        }
        return order.getId();
    }


    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteStockCheckDetailById(AuthPlatformUserInfo userInfo, Integer id) {

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpStockCheckDetail exist = selectOne(new EntityWrapper<ErpStockCheckDetail>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("删除库存盘点详情时，没有找到相关记录，id为："+id);
            return;
        }
        Integer productId = exist.getProductId();
        Integer productBatchId = exist.getProductBatchId();
        Integer repositoryId = exist.getRepositoryId();
        Integer companyId = exist.getCompanyId();
        BigDecimal expectNumber = exist.getExpectNumber();
        BigDecimal realNumber = exist.getRealNumber();
        BigDecimal d = realNumber.subtract(expectNumber);

        Wrapper<ErpStock> wrapper = new EntityWrapper<ErpStock>().eq("org_id", companyId)
                .eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0);
        if(!ObjectUtils.isEmpty(productBatchId)){
            wrapper.eq("product_batch_id", productBatchId);
        }else {
            wrapper.isNull("product_batch_id");
        }
        ErpStock existStock = erpStockService.selectOne(wrapper);
        if (!ObjectUtils.isEmpty(existStock)) {
            BigDecimal oldDNum = existStock.getDNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            // 旧调整值 减去 差值
            existStock.setDNum(oldDNum.subtract(d));
            // 旧现有量 减去 差值
            existStock.setStockNum(oldStockNum.subtract(d));

            if (!erpStockService.updateById(existStock)) {
                log.error("删除库存盘点失败，调整库存盘点调整值，现有量失败,调用{}的{}方法出错", "ErpStockCheckDetailServiceImpl", "deleteStockCheckDetailById()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_STOCK_CHECK_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }

        } else {

            log.info("删除库存盘点明细时，调整库存量，没有找到相关记录");

        }


        if (!updateById(new ErpStockCheckDetail().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除库存盘点失败，调用{}的{}方法出错", "ErpStockCheckDetailServiceImpl", "deleteStockCheckDetailById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_STOCK_CHECK_DETAIL_DELETE_FAILED);
        }


    }

    /**
     * 根据id查询库存盘点详情
     * @param id
     * @return
     */
    @Override
    public ErpStockCheckDetailDTO getStockCheckDetailById(Integer id) {
        ErpStockCheckDetailDTO dto = new ErpStockCheckDetailDTO();
        ErpStockCheckDetail d = selectOne(new EntityWrapper<ErpStockCheckDetail>().eq("id",id).eq("deleted",0));
        if (ObjectUtils.isEmpty(d)) {
            log.info("根据id查看库存盘点详情,没有找到相关数据，id为："+id);
            return dto;
        }
        dto = BeanUtils.copyProperties(d, ErpStockCheckDetailDTO.class);
        ErpProduct p = productService.selectById(d.getProductId());
        ErpProductBatch pb = productBatchService.selectById(d.getProductBatchId());
        if (!ObjectUtils.isEmpty(p)) {
            dto.setProductName(p.getName()).setUnit(p.getUnit());
        }
        if (!ObjectUtils.isEmpty(p) && !ObjectUtils.isEmpty(pb)) {
            dto.setProductAndBatch(p.getName()+"-"+pb.getBatchName());
        }else if(!ObjectUtils.isEmpty(p) && ObjectUtils.isEmpty(pb)) {
            dto.setProductAndBatch(p.getName());
        }
        dto.setStorehouse(commonMapper.getRepositoryNameById(d.getRepositoryId()));
        dto.setTestCode(commonMapper.getTestCodeById(d.getTestReportId()));
        return dto;
    }
}
