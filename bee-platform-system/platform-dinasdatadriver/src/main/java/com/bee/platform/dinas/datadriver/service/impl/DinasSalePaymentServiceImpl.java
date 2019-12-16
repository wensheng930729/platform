package com.bee.platform.dinas.datadriver.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasSalePaymentMapper;
import com.bee.platform.dinas.datadriver.dto.DinasSalePaymentDTO;
import com.bee.platform.dinas.datadriver.dto.DinasUrlDTO;
import com.bee.platform.dinas.datadriver.dto.SalePaymentListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrder;
import com.bee.platform.dinas.datadriver.entity.DinasSalePayment;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentListRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderService;
import com.bee.platform.dinas.datadriver.service.DinasSalePaymentService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 销售回款 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasSalePaymentServiceImpl extends ServiceImpl<DinasSalePaymentMapper, DinasSalePayment> implements DinasSalePaymentService {

    @Autowired
    private DinasSalePaymentMapper salePaymentMapper;

    @Autowired
    private DinasSaleOrderService saleOrderService;

    @Autowired
    private AppendixUtils appendixUtils;

    /**
     * @descriptin 添加销售回款
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, SalePaymentRQ rq) {
        //校验回款单号是否重复
        if (StringUtils.isNotBlank(rq.getCode())) {
            DinasSalePayment salePayment = this.selectOne(new EntityWrapper<>(new DinasSalePayment()
                    .setCode(rq.getCode())
                    .setCompanyId(userInfo.getOrgId())
                    .setDeleted(Status.FALSE.getKey())));
            if (Objects.nonNull(salePayment)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_PAYMENT_EXIST);
            }
        }
        DinasSalePayment salePayment = BeanUtils.copyProperties(rq, DinasSalePayment.class);
        salePayment.setCreateTime(new Date()).setCreateUser(userInfo.getId())
                .setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
                .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
        if (!this.insert(salePayment)) {
            log.error("添加销售回款失败,调用{}类{}方法出错","DinasSalePaymentServiceImpl","add()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        //更新销售合同中的已回款金额
        DinasSaleOrder dinasSaleOrder = saleOrderService.selectOne(new EntityWrapper<>(new DinasSaleOrder()
                .setId(rq.getOrderId())
                .setDeleted(Status.FALSE.getKey())));
        if (!ObjectUtils.isEmpty(dinasSaleOrder)) {
            //已付款金额
            BigDecimal payment = dinasSaleOrder.getPayment();
            if (!ObjectUtils.isEmpty(rq.getReceiveAmount())) {
                payment = payment.add(rq.getReceiveAmount());
                dinasSaleOrder.setPayment(payment);
                //可以金额
                BigDecimal availableAmount = dinasSaleOrder.getAvailableAmount().add(rq.getReceiveAmount());
                dinasSaleOrder.setAvailableAmount(availableAmount);
                saleOrderService.updateById(dinasSaleOrder);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, salePayment.getId());
    }

    /**
     * @descriptin 编辑销售回款
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, SalePaymentRQ rq) {
        if (!ObjectUtils.isEmpty(rq.getId())) {
            DinasSalePayment salePayment = this.selectOne(new EntityWrapper<>(new DinasSalePayment()
                    .setId(rq.getId()).setDeleted(Status.FALSE.getKey())));
            if (ObjectUtils.isEmpty(salePayment)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }

            //校验回款单号是否重复
            if (StringUtils.isNotBlank(rq.getCode())) {
                DinasSalePayment salePaymentExist = this.selectOne(new EntityWrapper<>(new DinasSalePayment()
                        .setCode(rq.getCode())
                        .setCompanyId(userInfo.getOrgId())
                        .setDeleted(Status.FALSE.getKey())));
                if (Objects.nonNull(salePaymentExist) && !rq.getId().equals(salePaymentExist.getId())) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_PAYMENT_EXIST);
                }
            }

            DinasSalePayment dinasSalePayment = BeanUtils.copyProperties(rq, DinasSalePayment.class);
            dinasSalePayment.setUpdateUser(userInfo.getId()).setUpdateTime(new Date())
                    .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
            if (!this.updateById(dinasSalePayment)) {
                log.error("编辑销售回款失败,调用{}类{}方法出错","DinasSalePaymentServiceImpl","update()");
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }

            //更新销售合同中的已回款金额
            DinasSaleOrder dinasSaleOrder = saleOrderService.selectOne(new EntityWrapper<>(new DinasSaleOrder()
                    .setId(rq.getOrderId()).setDeleted(Status.FALSE.getKey())));
            if (!ObjectUtils.isEmpty(dinasSaleOrder)) {
                //已回款金额
                BigDecimal payment = dinasSaleOrder.getPayment();
                BigDecimal newPayment = BigDecimal.ZERO;
                if (!ObjectUtils.isEmpty(rq.getReceiveAmount())) {
                    newPayment = rq.getReceiveAmount();
                }
                payment = payment.subtract(salePayment.getReceiveAmount()).add(newPayment);
                dinasSaleOrder.setPayment(payment);

                //可用金额
                BigDecimal availableAmount = dinasSaleOrder.getAvailableAmount().subtract(salePayment.getReceiveAmount()).add(newPayment);
                dinasSaleOrder.setAvailableAmount(availableAmount);
                saleOrderService.updateById(dinasSaleOrder);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getId());
    }

    /**
     * @descriptin 查询销售回款详情
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<DinasSalePaymentDTO> findInfo(Integer id) {
        DinasSalePaymentDTO salePayment = salePaymentMapper.findInfo(id);
        if (Objects.nonNull(salePayment) && StringUtils.isNotBlank(salePayment.getUrl())) {
            salePayment.setUrlList(JSONObject.parseArray(salePayment.getUrl(), DinasUrlDTO.class));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, salePayment);
    }

    /**
     * @descriptin 批量删除销售回款
     * @author xin.huang
     * @param rq
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq) {
        DinasSalePayment dinasSalePayment = new DinasSalePayment()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.update(dinasSalePayment, new EntityWrapper<DinasSalePayment>().in("id", rq.getIds()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getIds());
    }

    /**
     * @descriptin 查询销售回款列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<SalePaymentListDTO>> findList(AuthPlatformUserInfo userInfo, SalePaymentListRQ rq, Pagination pagination) {
        if (Objects.isNull(rq)) {
            rq = new SalePaymentListRQ();
        }
        rq.setCompanyId(userInfo.getOrgId());
        List<SalePaymentListDTO> salePaymentList = salePaymentMapper.findList(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, salePaymentList, PageUtils.transToPage(pagination));
    }
}
