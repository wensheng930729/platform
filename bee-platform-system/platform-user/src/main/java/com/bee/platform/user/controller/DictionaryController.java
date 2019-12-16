package com.bee.platform.user.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.DictionaryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-03-04
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "dict", tags = "字典相关接口")
@RequestMapping("/api/dict")
public class DictionaryController {

    @Autowired
    private DictionaryService dictionaryService;

    @ApiOperation(value = "获取字典信息")
    @RequestMapping(value = "/list", method = RequestMethod.GET)
    public ResponseResult getEnterpriseInfo(@RequestParam Integer type) {
        try {
            return dictionaryService.getDictionaryByType(type);
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

}

