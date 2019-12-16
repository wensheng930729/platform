package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.LearnDTO;
import com.bee.platform.user.entity.Learn;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-04
 */
public interface LearnService extends IService<Learn> {
	 /**
     * 创建学习指南
     * @param userInfo
     * @param title
     * @param content
     * @param type
     * @param depName
     * @return
     */
    ResponseResult addLearn(AuthPlatformUserInfo userInfo, String title, String content, int type, String depName);



    /**
     * 修改学习指南
     * @param userInfo
     * @param id
     * @param title
     * @param content
     * @param type
     * @param depName
     * @return
     */
    ResponseResult modifyLearn(AuthPlatformUserInfo userInfo, int id, String title, String content, int type, String depName);



    /**
     * 删除学习指南
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult deleteLearn(AuthPlatformUserInfo userInfo, int id);


    /**
     * 根据Id查询单条学习指南
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<LearnDTO> getLearnById(AuthPlatformUserInfo userInfo, int id);

    /**
     * 按标题搜索文章
     * @param userInfo
     * @param title
     * @return
     */
    ResponseResult<List<LearnDTO>> findLearnByTitle(AuthPlatformUserInfo userInfo, String title);


    /**
     * 管理员获取通过类型获取学习指南列表
     * @param userInfo
     * @param type
     * @param page
     * @return
     */
    ResponseResult<List<LearnDTO>> getAllByType(AuthPlatformUserInfo userInfo, int type, Page page);


    /**
     * 用户获取获取学习指南列表(没有条件的情况，只分页)
     * @param userInfo
     * @param page
     * @return
     */
    ResponseResult<List<LearnDTO>> getAll(AuthPlatformUserInfo userInfo, Page page);
}
