package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpProductBatchListDTO;
import com.bee.platform.datadriver.service.ErpProductBatchService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 产品批次分类项 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-07-12
 */
@Slf4j
@RestController
@RequestMapping("/erpProductBatch")
@Api(value = "ErpProductBatchController", tags = "erp产品批次相关接口")
public class ErpProductBatchController {

    @Autowired
    private ErpProductBatchService productBatchService;

    @GetMapping("/getByProductId/{productId}")
    @ApiOperation(value = "根据产品id查询产品批次列表")
    public ResponseResult<List<ErpProductBatchListDTO>> getProductBatchListByProductId(@PathVariable("productId") Integer productId) {
        return productBatchService.getProductBatchListByProductId(productId);
    }

}

