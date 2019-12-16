package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.entity.PlatformManagers;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 后台管理用户列表 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-28
 */
public interface MPlatformManagersMapper extends BaseMapper<PlatformManagers> {

    /**
     * @notes: 通过用户id获得用户名称
     * @Author: junyang.li
     * @Date: 8:59 2019/5/5
     * @params: [list] 用户角色id集
     * @return: java.util.List<com.bee.platform.user.entity.PlatformManagers>
     **/
    List<PlatformManagers> getManagerNameById( @Param("set") Set<Integer> set);

    /**
     * @notes: 查询修改人昵称
     * @Author: junyang.li
     * @Date: 11:47 2019/5/13
     * @param updateId : 修改人id
     * @return: java.lang.String
     */
    String getUpdatorNickename(Integer updateId);
}
