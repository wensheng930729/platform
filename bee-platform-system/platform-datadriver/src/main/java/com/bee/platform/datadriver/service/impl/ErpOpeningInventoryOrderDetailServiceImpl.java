package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpOpeningInventoryOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryNameDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpOpeningInventoryOrderDetailService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.datadriver.service.ErpStockService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 期初库存明细表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@Service
public class ErpOpeningInventoryOrderDetailServiceImpl extends ServiceImpl<ErpOpeningInventoryOrderDetailMapper, ErpOpeningInventoryOrderDetail> implements ErpOpeningInventoryOrderDetailService {

    @Autowired
    private ErpOpeningInventoryOrderDetailMapper inventoryOrderDetailMapper;
    private static Integer ZERO = 0;

    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private ErpProductService erpProductService;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    /**
     * @param id
     * @return
     */
    @Override
    public List<ErpOpeningInventoryOrderDetail> listOpeningInventoryOrderDetail(String id) {
        return inventoryOrderDetailMapper.selectList(new EntityWrapper<ErpOpeningInventoryOrderDetail>()
                .eq("code", id).and()
                .eq("deleted", Status.FALSE.getKey()));
    }

    /**
     * 保存期初库存明细
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOpeningInventoryOrderDetail(AuthPlatformUserInfo userInfo, ErpOpeningInventoryOrderDetailRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
        ErpOpeningInventoryOrderDetail order = BeanUtils.copyProperties(rq, ErpOpeningInventoryOrderDetail.class);
        if (ObjectUtils.isEmpty(rqId)) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            order.setModifierId(userId).setModifyTime(time);
        }

        // 保存期初数量到库存表
        Integer productId = rq.getProductId();
        String productName = rq.getProductName();
        Integer repositoryId = rq.getRepositoryId();
        Integer companyId = rq.getCompanyId();
        String companyName = rq.getCompanyName();
        BigDecimal quantity = rq.getQuantity();
        ErpProductCategoryNameDTO category = erpProductService.getProductCategoryName(productId);
        String categoryName = "";
        if (!ObjectUtils.isEmpty(category)) {
            categoryName = category.getCategoryName();
        }

        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id", companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        // 如果不存在则创建新记录
        if (ObjectUtils.isEmpty(existStock)) {
            ErpStock stock = new ErpStock()
                    .setOrgId(companyId)
                    .setCompanyName(companyName)
                    .setRepositoryId(repositoryId)
                    .setProductId(productId)
                    .setProductName(productName)
                    .setProductCategory(categoryName)
                    .setUnit(rq.getUnitOfMeasurement())
                    .setRepositoryName(rq.getStoreHouseName())
                    .setInitNum(quantity)
                    .setStockNum(quantity)
                    .setTestReportId(rq.getTestReportId())
                    .setTestReportCode(rq.getTestOrder());
            if (!erpStockService.insert(stock)) {
                log.error("保存期初库存明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);

            }
            // 如果存在记录则修改
        } else {
            // 如果是新添加的
            if (ObjectUtils.isEmpty(rqId)) {
                BigDecimal addInit = quantity.add(existStock.getInitNum());
                BigDecimal addStock = quantity.add(existStock.getStockNum());
                existStock.setInitNum(addInit);
                existStock.setStockNum(addStock);
            } else {
                // 如果是编辑
                ErpOpeningInventoryOrderDetail exist = selectById(rqId);
                if (!ObjectUtils.isEmpty(exist)) {
                    BigDecimal oldQuantity = exist.getQuantity();
                    BigDecimal oldInitNum = existStock.getInitNum();
                    BigDecimal oldStockNum = existStock.getStockNum();
                    existStock.setInitNum(oldInitNum.add(quantity).subtract(oldQuantity));
                    existStock.setStockNum(oldStockNum.add(quantity).subtract(oldQuantity));

                } else {

                    log.info("保存期初库存明细时，调整库存，找不到相关明细记录");
                }

            }

            if (!erpStockService.updateById(existStock)) {
                log.error("保存期初库存明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);
            }
        }


        if (!insertOrUpdate(order)) {
            log.error("保存期初库存明细失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DETAIL_SAVE_FAILED);
        }
        return order.getId();
    }

    /**
     * 根据id删除期初库存明细
     *
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteOpeningInventoryOrderDetailById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpOpeningInventoryOrderDetail exist = selectOne(new EntityWrapper<ErpOpeningInventoryOrderDetail>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("没有找到相关期初库存明细详情，id为："+id);
            return;
        }
        BigDecimal oldQuantity = exist.getQuantity();
        Integer productId = exist.getProductId();
        Integer repositoryId = exist.getRepositoryId();
        Integer companyId = exist.getCompanyId();
        // 查询库存记录
        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id", companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if (!ObjectUtils.isEmpty(existStock)) {
            // 有库存记录 修改记录
            BigDecimal oldInitNum = existStock.getInitNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            // 删除 减去old
            existStock.setInitNum(oldInitNum.subtract(oldQuantity));
            // 现有量 减去 old
            existStock.setStockNum(oldStockNum.subtract(oldQuantity));

            if (!erpStockService.updateById(existStock)) {
                log.error("删除期初库存明细失败，调整库存期初数量失败,调用{}的{}方法出错", "ErpOpeningInventoryOrderDetailServiceImpl", "deleteOpeningInventoryOrderDetailById()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }
        } else {

            log.info("删除期初明细时，调整库存量，没有找到相关记录");
        }

        if (!updateById(new ErpOpeningInventoryOrderDetail().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除期初库存明细失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderDetailServiceImpl", "deleteOpeningInventoryOrderDetailById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DETAIL_DELETE_FAILED);
        }

    }

    /**
     * 根据id查询期初库存详情
     * @param id
     * @return
     */
    @Override
    public ErpOpeningInventoryOrderDetailDTO getOpeningInventoryOrderDetailById(Integer id) {
        ErpOpeningInventoryOrderDetail d = selectOne(new EntityWrapper<ErpOpeningInventoryOrderDetail>().eq("id",id).eq("deleted",0));
        ErpOpeningInventoryOrderDetailDTO dto = new ErpOpeningInventoryOrderDetailDTO();
        if (ObjectUtils.isEmpty(d)) {
            log.info("根据id查看期初库存详情,没有找到相关数据，id为："+id);
            return dto;
        }
        Integer productId = d.getProductId();
        ErpProduct p = productService.selectById(productId);
        if (!ObjectUtils.isEmpty(p)) {
            d.setProductName(p.getName()).setUnitOfMeasurement(p.getUnit());
        }
        d.setStoreHouseName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
        d.setTestOrder(commonMapper.getTestCodeById(d.getTestReportId()));

        dto = BeanUtils.copyProperties(d, ErpOpeningInventoryOrderDetailDTO.class);


        return dto;
    }
}
