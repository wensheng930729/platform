package com.bee.platform.dinas.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasSaleAdjustDetailMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasSaleAdjustMapper;
import com.bee.platform.dinas.datadriver.dto.SaleAdjustDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleAdjust;
import com.bee.platform.dinas.datadriver.entity.DinasSaleAdjustDetail;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrderDetail;
import com.bee.platform.dinas.datadriver.rq.SaleAdjustDetailRQ;
import com.bee.platform.dinas.datadriver.rq.SaleAdjustRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleAdjustDetailService;
import com.bee.platform.dinas.datadriver.service.DinasSaleAdjustService;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderDetailService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;

/**
 * <p>
 * 销售调价主表 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Service
public class DinasSaleAdjustServiceImpl extends ServiceImpl<DinasSaleAdjustMapper, DinasSaleAdjust> implements DinasSaleAdjustService {

    @Autowired
    private DinasSaleAdjustDetailService adjustDetailService;

    @Autowired
    private DinasSaleAdjustDetailMapper adjustDetailMapper;

    @Autowired
    private DinasSaleOrderDetailService saleOrderDetailService;

    @Autowired
    private AppendixUtils appendixUtils;

    /**
     * @descriptin 添加销售合同调价函
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo, SaleAdjustRQ rq) {
        if (!ObjectUtils.isEmpty(rq)) {
            //校验产品和规格是否重复
            List<SaleAdjustDetailRQ> saleAdjustDetailList = rq.getSaleAdjustDetailList();
            if (!CollectionUtils.isEmpty(saleAdjustDetailList)) {
                Map<Integer, Integer> productMap = new HashMap<>(saleAdjustDetailList.size());
                for (SaleAdjustDetailRQ adjust : saleAdjustDetailList) {
                    Integer specId = productMap.get(adjust.getProductId());
                    if (Objects.nonNull(specId) && specId.equals(adjust.getProductSpecId())) {
                        return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_PRODUCT_REPEAT);
                    }
                    productMap.put(adjust.getProductId(), adjust.getProductSpecId());
                }
            }

            //添加调价函基本信息
            DinasSaleAdjust dinasSaleAdjust = new DinasSaleAdjust().setOrderId(rq.getOrderId())
                    .setUrl(appendixUtils.getJsonStr(rq.getUrlList()))
                    .setAdjustDate(rq.getAdjustDate()).setCreateTime(new Date())
                    .setCreateUser(userInfo.getId());
            this.insert(dinasSaleAdjust);

            //添加调价函明细信息
            batchInsertDetail(dinasSaleAdjust.getId(), rq.getSaleAdjustDetailList());

            //修改销售合同明细中的价格
            updateAdjustPrice(rq.getOrderId(), saleAdjustDetailList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 编辑销售合同调价函
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, SaleAdjustRQ rq) {
        if (!ObjectUtils.isEmpty(rq)) {
            if (ObjectUtils.isEmpty(rq.getId())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
            }
            DinasSaleAdjust dinasSaleAdjust = this.selectById(rq.getId());
            if (ObjectUtils.isEmpty(dinasSaleAdjust)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }

            //校验产品和规格是否重复
            List<SaleAdjustDetailRQ> saleAdjustDetailList = rq.getSaleAdjustDetailList();
            if (!CollectionUtils.isEmpty(saleAdjustDetailList)) {
                Map<Integer, Integer> productMap = new HashMap<>(saleAdjustDetailList.size());
                for (SaleAdjustDetailRQ adjust : saleAdjustDetailList) {
                    Integer specId = productMap.get(adjust.getProductId());
                    if (Objects.nonNull(specId) && specId.equals(adjust.getProductSpecId())) {
                        return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_PRODUCT_REPEAT);
                    }
                    productMap.put(adjust.getProductId(), adjust.getProductSpecId());
                }
            }

            //编辑调价函基本信息
            DinasSaleAdjust aleAdjust = new DinasSaleAdjust().setOrderId(rq.getOrderId())
                    .setUrl(appendixUtils.getJsonStr(rq.getUrlList()))
                    .setUpdateTime(new Date()).setAdjustDate(rq.getAdjustDate())
                    .setUpdateUser(userInfo.getId()).setId(rq.getId());
            this.updateById(aleAdjust);
            //删除调价函基本信息
            adjustDetailService.update(new DinasSaleAdjustDetail().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<>(new DinasSaleAdjustDetail().setAdjustId(rq.getId())));
            //添加调价函明细信息
            batchInsertDetail(rq.getId(), rq.getSaleAdjustDetailList());

            //修改销售合同明细中的价格
            updateAdjustPrice(rq.getOrderId(), saleAdjustDetailList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 根据调价函id查询调价函详情列表
     * @author xin.huang
     * @param adjustId
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<List<SaleAdjustDetailDTO>> getAdjustDetails(Integer adjustId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, adjustDetailMapper.findList(adjustId));
    }

    /**
     * @descriptin 批量添加调价函明细
     * @author xin.huang
     * @param adjustId
     * @param saleAdjustDetailList
     * @date 2019/8/14
     * @return
     */
    private void batchInsertDetail(Integer adjustId, List<SaleAdjustDetailRQ> saleAdjustDetailList) {
        if (!CollectionUtils.isEmpty(saleAdjustDetailList)) {
            List<DinasSaleAdjustDetail> adjustDetails = BeanUtils.assemble(DinasSaleAdjustDetail.class, saleAdjustDetailList);
            adjustDetails.forEach(adjust -> {
                adjust.setAdjustId(adjustId).setCreateTime(new Date());
            });
            adjustDetailService.insertBatch(adjustDetails);
        }
    }

    /**
     * @descriptin 更新销售合同明细中的产品价格
     * @author xin.huang
     * @param
     * @date 2019/8/15
     * @return
     */
    private void updateAdjustPrice(Integer orderId, List<SaleAdjustDetailRQ> saleAdjustDetailList) {
        List<DinasSaleOrderDetail> saleOrderDetails = saleOrderDetailService.selectList(new EntityWrapper<>(new DinasSaleOrderDetail()
                .setOrderId(orderId).setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isEmpty(saleOrderDetails) || CollectionUtils.isEmpty(saleAdjustDetailList)) {
            return;
        }
        for (SaleAdjustDetailRQ adjustDetail : saleAdjustDetailList) {
            saleOrderDetailService.update(new DinasSaleOrderDetail()
                            .setPrice(adjustDetail.getPriceAfter())
                            .setTaxPrice(adjustDetail.getTaxPriceAfter()),
                    new EntityWrapper<>(new DinasSaleOrderDetail()
                            .setOrderId(orderId)
                            .setProductId(adjustDetail.getProductId())
                            .setProductSpecId(adjustDetail.getProductSpecId())
                            .setDeleted(Status.FALSE.getKey())));
        }
    }
}
