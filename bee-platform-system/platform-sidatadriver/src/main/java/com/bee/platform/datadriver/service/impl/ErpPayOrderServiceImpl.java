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
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.datadriver.dao.mapper.ErpCodeMapper;
import com.bee.platform.datadriver.dao.mapper.ErpOperationLogMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPayOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderMapper;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpPayOrderSaveRQ;
import com.bee.platform.datadriver.rq.PayOrderRQ;
import com.bee.platform.datadriver.service.ErpPayOrderService;
import com.bee.platform.datadriver.support.OperateType;
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
    private ErpOperationLogMapper operationLogMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询付款单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpPayOrder> listErpPayOrder(Pagination pagination, PayOrderRQ rq, AuthPlatformUserInfo userInfo) {
        Wrapper wrapper = new EntityWrapper<ErpPayOrder>().eq("deleted", Status.FALSE.getKey()).and()
                .eq("company",userInfo.getOrgId());
        if (!ObjectUtils.isEmpty(rq)){
            if (!StringUtils.isEmpty(rq.getPurchaseOrderNo())){
                wrapper.and().like("purchase_order_no",rq.getPurchaseOrderNo());
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
            // 插入操作日志
            insertOperationLog(payOrder,userInfo, OperateType.ADD.getMsg());
        }else {
            if (payOrderMapper.updateById(payOrder
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("修改付款单失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PAY_UPDATE_FAILED);
            }
            // 插入操作日志
            insertOperationLog(payOrder,userInfo,OperateType.EDIT.getMsg());
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
                .eq("deleted",Status.FALSE.getKey())
                .orderBy("pay_date",false));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }
    /**
     * 插入操作日志
     * @param payOrder
     * @param userInfo
     * @param msg
     */
    private void insertOperationLog(ErpPayOrder payOrder, AuthPlatformUserInfo userInfo, String msg) {
        // 插入操作日志
        if(operationLogMapper.insert(new ErpOperationLog()
                .setBusinessId(payOrder.getId())
                .setCompanyId(userInfo.getOrgId())
                .setBusinessType(EnumBusinessType.PURCHASE_PAYMENT.getCode())
                .setOperateMsg(msg)
                .setOperator(userInfo.getId())
                .setOperatorName(userInfo.getName())
                .setOperateTime(new Date())) <= ZERO){
            log.error("新增付款单操作日志失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_LOG_ADD_FAILED);
        }
    }
}
