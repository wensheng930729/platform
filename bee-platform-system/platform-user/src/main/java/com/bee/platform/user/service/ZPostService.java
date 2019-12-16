package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.business.dto.PostListDTO;
import com.bee.platform.business.rq.PostAddRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.entity.ZPost;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-25
 */
public interface ZPostService extends IService<ZPost> {

    /**
     * 条件查询职位列表
     * @param userInfo
     * @param name
     * @param departmentId
     * @return
     */
    ResponseResult<List<PostListDTO>> getPostList(AuthPlatformUserInfo userInfo, String name, Integer departmentId, Page page);

    /**
     * 编辑 新增职位
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult editPost(AuthPlatformUserInfo userInfo, PostAddRQ rq);

    /**
     * 职位删除
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult deletePost(AuthPlatformUserInfo userInfo, Integer id);


    /**
     * 职位验证名称唯一性
     * @param userInfo
     * @param name
     * @param departmentId
     * @return
     */
    ResponseResult<Integer> postNameCheck(AuthPlatformUserInfo userInfo, String name, Integer departmentId);
    /**
     *  根据部门id查询 部门下的职位
     * @param departmentId
     * @return
     */
    ResponseResult<List<ZPost>> getByDepartment(Integer departmentId);

    PostListDTO getPostById(Integer id);
}
