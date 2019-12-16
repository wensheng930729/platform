package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.rq.OrderQueryRQ;
import com.bee.platform.datadriver.service.ErpCodeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 码表 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-02
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpCode", tags = "erp码表相关接口")
@RequestMapping("/erpCode")
public class ErpCodeController {

    @Autowired
    private ErpCodeService erpCodeService;

    @GetMapping("/listErpCode")
    @ApiOperation(value = "查询码表列表")
    public ResponseResult<List<ErpCode>> listErpCode(HttpServletRequest request, @RequestParam String code){
        List<ErpCode> list = erpCodeService.listErpCode(code);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

}

