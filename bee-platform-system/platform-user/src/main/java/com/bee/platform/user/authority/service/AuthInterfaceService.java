package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthInterfaceDto;
import com.bee.platform.user.authority.entity.AuthInterface;
import com.bee.platform.user.authority.rq.AuthInterfaceRQ;
import com.bee.platform.user.authority.rq.AuthInterfaceSelectRQ;

import java.io.InputStream;
import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
public interface AuthInterfaceService extends IService<AuthInterface> {

    /**
     * @Description 条件查询接口列表
     * @Param authInterfaceRQs
     * @Date 2019/5/20 16:13
     * @Author xin.huang
     * @Return
     */
    ResponseResult<List<AuthInterfaceDto>> getList(AuthInterfaceSelectRQ authInterfaceSelectRQ, Page page);

    /**
     * @Description 添加接口
     * @Param authInterfaceRQs
     * @Date 2019/5/20 14:35
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> add(AuthInterfaceRQ authInterfaceRQs);
    
    /**
     * @Description 根据路由、URL、请求方式添加接口
     * @Param authInterfaceRQs
     * @Date 2019/06/08  12:17
     * @Author zhigang.zhou
     */
    ResponseResult<ResCodeEnum> addByRouterUrlMethod(AuthInterfaceRQ authInterfaceRQs);

    /**
     * @Description 更新接口
     * @Param id
     * @Param authInterfaceRQs
     * @Date 2019/5/20 15:01
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> update(Integer id, AuthInterfaceRQ authInterfaceRQs);

    /**
     * @Description 批量删除接口
     * @Param ids
     * @Date 2019/5/20 15:17
     * @Author xin.huangss
     * @Return
     */
    ResponseResult<ResCodeEnum> batchDelete(String ids);
    /**
     * @notes: excel批量导入接口
     * @Author: junyang.li
     * @Date: 14:42 2019/5/28
     * @param in :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> excelImport(InputStream in);

    /**
     * 根据子系统查询所有接口-后台
     * @param subSys
     * @param beeRouter
     * @return
     */
    List<AuthInterfaceDto> listInterfacesBySubSys(String subSys, String beeRouter);

    /**
     *
     * @param id
     * @return
     */
    ResponseResult<AuthInterfaceDto> getInterfaceDetail(String id);
}
