package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpCodeMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPayOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderMapper;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.entity.ErpPayOrder;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.rq.ErpPayOrderSaveRQ;
import com.bee.platform.datadriver.rq.PayOrderRQ;
import com.bee.platform.datadriver.service.ErpPayOrderService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 付款单 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpPayOrderServiceImpl extends ServiceImpl<ErpPayOrderMapper, ErpPayOrder> implements ErpPayOrderService {

    @Autowired
    private ErpPayOrderMapper payOrderMapper;
    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpCodeMapper codeMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询付款单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpPayOrder> listErpPayOrder(Pagination pagination, PayOrderRQ rq, Integer companyId) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpPayOrderServiceImpl", "listErpPayOrder");
            return Lists.newArrayList();
        }
        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        Wrapper wrapper = new EntityWrapper<ErpPayOrder>().eq("deleted", Status.FALSE.getKey());
        if (!ObjectUtils.isEmpty(rq)){
            if (!StringUtils.isEmpty(rq.getPurchaseOrderNo())){
                wrapper.and().like("purchase_order_no",rq.getPurchaseOrderNo());
            }
            if (rq.getCompany() != null){
                wrapper.and().eq("company",rq.getCompany());
            }else {
                wrapper.and().in("company",enterpriseIds);
            }
            if (!StringUtils.isEmpty(rq.getCode())){
                wrapper.and().like("code",rq.getCode());
            }
            if (!StringUtils.isEmpty(rq.getSupplyName())){
                wrapper.and().like("supply_name",rq.getSupplyName());
            }
            if (!StringUtils.isEmpty(rq.getPayStartdate())){
                wrapper.and().gt("pay_date",rq.getPayStartdate() + " 00:00:00");
            }
            if (!StringUtils.isEmpty(rq.getPayEnddate())){
                wrapper.and().lt("pay_date",rq.getPayEnddate() + " 23:59:59");
            }
        }else {
            wrapper.and().in("company",enterpriseIds);
        }
        wrapper.orderBy("pay_date",false);
        return payOrderMapper.selectPage(pagination,wrapper);
    }

    /**
     * 删除付款单
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<Integer> deleteErpPayOrder(AuthPlatformUserInfo userInfo, Integer id) {
        ErpPayOrder payOrder = payOrderMapper.selectById(new ErpPayOrder().setDeleted(Status.FALSE.getKey()).setId(id));

        if (payOrderMapper.updateById(new ErpPayOrder()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setUpdateUser(userInfo.getId())
                .setUpdateTime(new Date())) <= ZERO){
            log.error("删除付款单失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","deleteErpPayOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PAY_DELETE_FAILED);
        }
        List<ErpPayOrder> payOrderList = payOrderMapper.selectList(new EntityWrapper<>(new ErpPayOrder()
                .setDeleted(Status.FALSE.getKey())
                .setPurchaseOrder(payOrder.getPurchaseOrder())));
        if (CollectionUtils.isEmpty(payOrderList)){
            // 修改订单付款状态
            if (purchaseOrderMapper.update(new ErpPurchaseOrder()
                    .setPayState(Status.FALSE.getKey())
                    .setUpdateTime(new Date()),new EntityWrapper<ErpPurchaseOrder>()
                    .eq("id",payOrder.getPurchaseOrder()).and()
                    .eq("deleted",Status.FALSE.getKey())) <= ZERO){
                log.error("修改订单付款状态失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","deleteErpPayOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_INVOICE_STATE_UPDATE_FAILED);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }

    /**
     * 确认付款单
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> confirmErpPayOrder(AuthPlatformUserInfo userInfo, String id) {
        if (payOrderMapper.updateById(new ErpPayOrder()
                .setId(Integer.valueOf(id)).setState(Status.TRUE.getKey()).setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("确认付款单失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","confirmErpPayOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存/编辑付款单
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> saveErpPayOrder(AuthPlatformUserInfo userInfo, ErpPayOrderSaveRQ rq) {
        ErpPayOrder payOrder = BeanUtils.copyProperties(rq,ErpPayOrder.class);
        if (rq.getId() == null){
            if (!ObjectUtils.isEmpty(payOrderMapper.selectOne(new ErpPayOrder()
                    .setCode(rq.getCode())
                    .setCompany(rq.getCompany())
                    .setDeleted(Status.FALSE.getKey())))){
                log.error("付款单编号重复", "ErpPayOrderServiceImpl", "saveErpPayOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_CODE_REPEAT);
            }
            if (payOrderMapper.insert(payOrder
                    .setState(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("新增付款单失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PAY_ADD_FAILED);
            }
        }else {
            if (payOrderMapper.updateById(payOrder
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("修改付款单失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PAY_UPDATE_FAILED);
            }
        }
        // 修改订单付款状态
        if (purchaseOrderMapper.update(new ErpPurchaseOrder()
                .setPayState(Status.TRUE.getKey())
                .setUpdateTime(new Date()),new EntityWrapper<ErpPurchaseOrder>()
                .eq("id",rq.getPurchaseOrder()).and().eq("deleted",Status.FALSE.getKey())) <= ZERO){
            log.error("修改订单付款失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PAY_UPDATE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,payOrder.getId());
    }

    /**
     * 根据id查询付款单
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpPayOrder> getErpPayOrderById(AuthPlatformUserInfo userInfo, Integer id) {
        ErpPayOrder payOrder = payOrderMapper.selectById(new ErpPayOrder()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        /*payOrder.setPayMethod(codeMapper.selectOne(new ErpCode().setCode("pay_method")
                .setValue(payOrder.getPayMethod())).getName());*/
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,payOrder);
    }

    /**
     * 根据采购单id查询付款单
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<List<ErpPayOrder>> getErpPayOrderByPurchase(AuthPlatformUserInfo userInfo, Integer id) {
        List<ErpPayOrder> list = payOrderMapper.selectList(new EntityWrapper<ErpPayOrder>()
                .eq("purchase_order",id).and()
                .eq("deleted",Status.FALSE.getKey()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }
}
