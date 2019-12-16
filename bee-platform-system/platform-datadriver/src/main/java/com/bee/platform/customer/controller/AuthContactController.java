package com.bee.platform.customer.controller;


import com.bee.platform.customer.service.AuthContactService;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authContact", tags = "联系人相关的接口")
@RequestMapping("/authContact")
public class AuthContactController {

    @Autowired
    private AuthContactService authContactService;

//    @ApiOperation(value = "联系人查询",notes = "联系人查询")
//    @RequestMapping(value = "/get", method = RequestMethod.GET)
//    public ResponseResult getAuthContactById(@RequestBody AuthContactSelectRQ rq, Page page) {
//        if (Objects.isNull(rq)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
//        }
//        return authContactService.getList(rq,page);
//    }


}
