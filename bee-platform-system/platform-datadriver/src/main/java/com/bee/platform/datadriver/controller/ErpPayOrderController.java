package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.entity.ErpPayOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpPayOrderSaveRQ;
import com.bee.platform.datadriver.rq.PayOrderRQ;
import com.bee.platform.datadriver.service.ErpPayOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 付款单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPayOrder", tags = "erp付款单相关接口")
@RequestMapping("/erpPayOrder")
public class ErpPayOrderController {

    @Autowired
    private ErpPayOrderService payOrderService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/listErpPayOrder")
    @ApiOperation(value = "分页查询付款单列表")
    public ResponseResult<List<ErpPayOrder>> listErpPayOrder(HttpServletRequest request, @RequestBody PayOrderRQ rq , Page page){
        /*String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        if (StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }*/
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(rq.getCompany())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpPayOrder> list = payOrderService.listErpPayOrder(pagination,rq,companyId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/getErpPayOrderById")
    @ApiOperation(value = "根据id查询付款单")
    public ResponseResult<ErpPayOrder> getErpPayOrderById(HttpServletRequest request, Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.getErpPayOrderById(userInfo,id);
    }

    @GetMapping("/getErpPayOrderByPurchase")
    @ApiOperation(value = "根据采购单id查询付款单")
    public ResponseResult<List<ErpPayOrder>> getErpPayOrderByPurchase(HttpServletRequest request, Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.getErpPayOrderByPurchase(userInfo,id);
    }

    @PostMapping("/deleteErpPayOrder")
    @Log(businessType= EnumBusinessType.PURCHASE_PAYMENT, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除付款单")
    public ResponseResult<Integer> deleteErpPayOrder(HttpServletRequest request, Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.deleteErpPayOrder(userInfo,id);
    }

    @GetMapping("/confirmErpPayOrder")
    @ApiOperation(value = "确认付款单")
    public ResponseResult<ResCodeEnum> confirmOpeningInventoryOrder(HttpServletRequest request, String id){
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.confirmErpPayOrder(simpleUserInfo,id);
    }

    @PostMapping("/saveErpPayOrder")
    @Log(businessType= EnumBusinessType.PURCHASE_PAYMENT, operateType = OperateType.EDIT)
    @ApiOperation(value = "保存/编辑付款单")
    public ResponseResult<Integer> saveErpPayOrder(HttpServletRequest request, @RequestBody ErpPayOrderSaveRQ rq){
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.saveErpPayOrder(simpleUserInfo,rq);
    }

    @PostMapping("/updateErpPayOrder")
    @Log(businessType= EnumBusinessType.PURCHASE_PAYMENT, operateType = OperateType.EDIT)
    @ApiOperation(value = "保存/编辑付款单")
    public ResponseResult<Integer> updateErpPayOrder(HttpServletRequest request, @RequestBody ErpPayOrderSaveRQ rq){
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return payOrderService.saveErpPayOrder(simpleUserInfo,rq);
    }

}

