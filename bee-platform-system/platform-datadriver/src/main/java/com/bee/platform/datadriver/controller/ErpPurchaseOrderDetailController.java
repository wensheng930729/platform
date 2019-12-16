package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseOrderDetail;
import com.bee.platform.datadriver.rq.PurchaseOrderDetailSaveRQ;
import com.bee.platform.datadriver.service.ErpPurchaseOrderDetailService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 采购单明细 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPurchaseOrderDetail", tags = "erp采购订单明细相关接口")
@RequestMapping("/erpPurchaseOrderDetail")
public class ErpPurchaseOrderDetailController {

    @Autowired
    private ErpPurchaseOrderDetailService purchaseOrderDetailService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @GetMapping("/listErpPurchaseOrderDetail")
    @ApiOperation(value = "分页查询采购订单明细列表")
    public ResponseResult<List<ErpPurchaseOrderDetailDTO>> listErpPurchaseOrderDetail(HttpServletRequest request, @RequestParam String id , Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpPurchaseOrderDetailDTO> list = purchaseOrderDetailService.listErpPurchaseOrderDetail(pagination,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/deleteErpPurchaseOrderDetail")
    @ApiOperation(value = "删除采购订单明细")
    public ResponseResult<ResCodeEnum> deleteErpPurchaseOrderDetail(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderDetailService.deleteErpPurchaseOrderDetail(userInfo,id);
    }

    @PostMapping("/saveErpPurchaseOrderDetail")
    @ApiOperation(value = "保存采购订单明细")
    public ResponseResult<ResCodeEnum> saveErpPurchaseOrderDetail(HttpServletRequest request, @RequestBody PurchaseOrderDetailSaveRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderDetailService.saveErpPurchaseOrderDetail(userInfo,rq);
    }

}

